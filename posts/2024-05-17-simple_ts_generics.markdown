---
title: "Building A Frontend Component Framework With TypeScript: Part ἄλφα - Setting Up an NPM Project"
---

### Building A Frontend Component Framework With TypeScript: Part ἄλφα - Setting up an NPM project

In this blog series, we will be building a simple component based frontend framework entirely in TypeScript. This series will guide you through framework design, focusing on modular and reusable components.

**Why TypeScript?**

- **Strong Typing**: TypeScript's strong typing system helps catch errors early in the development process, making it ideal for components intended for wide reuse in various parts of an application or even across projects.
- **Enhanced Code Quality and Readability**: The type annotations and compile-time checks lead to more readable and maintainable code, crucial for components that need to be easily understood and modified by different developers over time.

We will not use TypeScript on the backend since the server setup is straightforward, and the framework is designed to be backend-agnostic, enabling deployment on any server environment. A typical production deployment might involve platforms like Netlify, which are geared towards hosting static sites and require no server-side processing.

### Prerequisites

The reader will need to have `node` and `npm` installed in order to follow along. View https://nodejs.org/en for more info. I'm using `v20.12.2`.


Also examples are in a MacOS environment and have not been tested in other environments, however other UNIX environments like Linux should work.

### Setting Up The Project

First lets make a directory.

``` bash
mkdir simple-ts-framework
cd simple-ts-framework
npm init -y
```

Then we shall install a few dependencies:

Typescript is the language we are using.

Express is used to serve pages.

Express is a dev dependency for now, as we might deploy this using amplify or something similar for static sites. Also, we won't use TypeScript on the backend, to simplify the code and reduce dependencies.

``` bash
npm install typescript 
npm install express --save-dev
```

Create a `server` directory with a JavaScript file:

``` bash
mkdir server
touch server/index.js
```

And add this to `index.js`

``` javascript
const express = require('express')

const app = express()
const port = 3000

app.use(express.static('public'))

app.listen(port, function() {
  console.log('Server running at http://localhost:' + port)
})
```

Create a `public` directory with the HTML and TypeScript file:

``` bash
mkdir public
touch public/index.html
```

Add this to index.html:

``` html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>TypeScript Generics</title>
</head>
<body>
  <h1>Hello, TypeScript!</h1>
  <script src="lib.js"></script>
</body>
</html>
```

Create a `src` directory:

``` bash
mkdir src
touch src/lib.ts
```

And add this to `lib.ts`:

``` javascript
document.body.innerHTML += "<p>Added by TypeScript!</p>"
```

Of course, we will need to initialize the typescript config file:

``` bash
npx tsc --init
```

And you can replace the contents with this:

``` json
{
  "compilerOptions": {
    "target": "es6",
    "module": "commonjs",
    "outDir": "./public",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true
  },
  "include": ["src/lib.ts"],
  "exclude": ["node_modules", "dist"]
}
```

Add these scripts to `package.json`:

``` json
"scripts": {
  "build": "tsc -p tsconfig.json",
  "watch": "tsc -p tsconfig.json --watch",
  "start": "node server/index.js"  
}
```

To test everything is working, run `npm run start` in one bash tab, and `npm run watch` in another bash tab.

Change `lib.ts` to something else, for example `document.body.innerHTML += "<p>Pirates!</p>"` and refresh to browser to see that the changes have taken effect.

If it's not working, review the previous steps to get it working before proceeding.

Finally, if you are really stuck, please review the repo for hints: https://github.com/tim-br/simple-ts-framework/tree/setting-up-a-ts-project