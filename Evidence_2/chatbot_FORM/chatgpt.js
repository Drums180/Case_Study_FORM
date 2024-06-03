const OpenAI = require("openai/index.mjs").default;
const dotenv = require("dotenv");
dotenv.config();

const openai = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

async function fetchChatGPTResponse(prompt) {
  try {
    const completion = await openai.chat.completions.create({
      messages: [
        { role: "system", content: "You are a helpful assistant" },
        { role: "user", content: prompt },
      ],
      model: "gpt-3.5-turbo",
    });
    return completion.choices[0].message.content;
  } catch (error) {
    console.error("Error fetching response from OpenAI:", error);
    return "Lo siento, no pude procesar tu solicitud.";
  }
}

module.exports = { fetchChatGPTResponse };
