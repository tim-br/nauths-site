---
title: "Building A Frontend Component Framework With TypeScript: Part I - Setting Up an NPM Project"
---

### Building A Frontend Component Framework With TypeScript: Part I - Setting up an NPM project

In this blog series, we will be building a simple component based frontend framework entirely in TypeScript. This series will guide you through framework design, focusing on modular and reusable components.

**Important Security Notice**: Please be aware that the code provided here is intended for demonstration purposes only and represents a basic implementation of a modern web framework. As such, it may be susceptible to common security vulnerabilities, including cross-site scripting (XSS) attacks. I strongly recommend not running this code in any production environment.

**Why TypeScript?**

- **Strong Typing**: TypeScript's strong typing system helps catch errors early in the development process, making it ideal for components intended for wide reuse in various parts of an application or even across projects.
- **Enhanced Code Quality and Readability**: The type annotations and compile-time checks lead to more readable and maintainable code, crucial for components that need to be easily understood and modified by different developers over time.

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

``` bash
npm install typescript parcel-bundler
```

Typescript is the language we are using and we're using parcel to bundle the typescript appropriately.

Create an html file:

``` bash
touch index.html
```

Add this to index.html:

``` html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Simple Typescript Framework</title>
</head>
<body>
  <h1>Hello, TypeScript!</h1>
  <script src="src/app.ts"></script>
</body>
</html>
```

Create a `src` directory, and add `app.ts` to the directory:

``` bash
mkdir src
touch src/app.ts
```

And add this to `app.ts`:

``` javascript
document.body.innerHTML += "<p>Added by TypeScript!</p>"
```

Add these scripts to `package.json`:

``` json
"scripts": {
    "start": "parcel index.html"
}
```

To test everything is working, run `npm run start`.

Then you can view the app at http://localhost:1234 and ensure that you view the sentence "Added by TypeScript!".

Change `app.ts` to something else, for example `document.body.innerHTML += "<p>Pirates!</p>"` see that the changes have taken effect (refreshing the browser is not necessary).

If it's not working, review the previous steps to get it working before proceeding.

Finally, if you are really stuck, please review the repo at the `setting-up-a-ts-project` branch for hints: https://github.com/tim-br/simple-ts-framework/tree/setting-up-a-ts-project.

Next: [Rendering Simple Components](https://nauths.io/posts/2024-05-18-simple_ts_framework.html)!