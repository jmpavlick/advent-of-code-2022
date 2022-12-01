import fs from 'fs';
import elm from './main.js';

const inputDir = "./input/";

fs.readdir(inputDir, (_, files) => {
    const filename = files.reverse()[0];
    console.log("today's file is " + filename);

    fs.readFile(inputDir + filename, "UTF-8", (_, input) => {
        console.log("read file " + filename + " OK");

        const { Elm } = elm;

        const app = Elm.Main.init({
            flags : {
                filename : filename,
                input : input
            }
        });
    });
});