const fs = require("fs");
const csv = require("csv-parser");
const makeWASocket = require("@whiskeysockets/baileys").default;
const {
  useMultiFileAuthState,
  DisconnectReason,
} = require("@whiskeysockets/baileys");
const { fetchChatGPTResponse } = require("./chatgpt.js");
const dotenv = require("dotenv");
dotenv.config();

const qnaFilePath = "databases/chatbot/q&a.csv";
let qnaData = [];

// Leer el archivo CSV y cargar los datos en memoria
fs.createReadStream(qnaFilePath)
  .pipe(csv())
  .on("data", (row) => {
    qnaData.push(row);
  })
  .on("end", () => {
    console.log("CSV file successfully processed");
    connectionLogic();
  });

async function connectionLogic() {
  const { state, saveCreds } = await useMultiFileAuthState("auth_info_baileys");
  const sock = makeWASocket({
    printQRInTerminal: true,
    auth: state,
  });

  sock.ev.on("connection.update", async (update) => {
    const { connection, lastDisconnect, qr } = update || {};
    if (qr) console.log(qr);
    if (
      connection === "close" &&
      lastDisconnect?.error?.output?.statusCode !== DisconnectReason.loggedOut
    ) {
      connectionLogic();
    }
  });

  sock.ev.on("messages.upsert", async (messageInfoUpsert) => {
    const message = messageInfoUpsert.messages[0];
    if (!message.key.fromMe && message.message.conversation) {
      const userMessage = message.message.conversation;
      const response = await getResponse(userMessage);
      await sock.sendMessage(message.key.remoteJid, { text: response });
    }
  });

  sock.ev.on("creds.update", saveCreds);
}

async function getResponse(userMessage) {
  const welcomeMessage =
    "¡Hola! Somos FORM 📦, una empresa dedicada a la fabricación de cartón para autopartes. ¿En qué podemos ayudarte hoy?\n\n" +
    "1. Prestaciones 💼\n" +
    "2. Documentos requeridos 📄\n" +
    "3. Ubicación 📍\n" +
    "4. Transporte 🚌\n" +
    "5. Actividad de la empresa 🏭\n" +
    "6. Experiencia requerida 💼\n" +
    "7. Examen médico 🩺\n" +
    "8. Contratación inmediata 🚀\n" +
    "9. Semana de nómina 📅\n" +
    "10. Sueldo e impuestos 💵\n" +
    "11. Comedor subsidiado 🍽️\n" +
    "12. Tarjeta de pago 💳\n" +
    "13. Contratación directa 📝\n" +
    "14. Entrevista 🗓️\n";

  if (!isNaN(userMessage) && userMessage >= 1 && userMessage <= 14) {
    const index = userMessage - 1;
    return qnaData[index].Respuesta;
  } else {
    const chatGptResponse = await fetchChatGPTResponse(userMessage);
    return chatGptResponse;
  }
}

module.exports = { getResponse };
