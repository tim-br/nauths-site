---
title: "Building A Frontend Component Framework With TypeScript: Part II - Rendering Simple Components"
---

### Building A Frontend Component Framework With TypeScript: Part II - Rendering Simple Components

In this part of the blog series, we will begin defining a few types and attempting to render them in the browser.

If you skipped the previous posts, you can follow along from the repo at the `setting-up-a-ts-project` branch: https://github.com/tim-br/simple-ts-framework/tree/setting-up-a-ts-project.

### An interface

Let's create a new file called `lib.ts` in the same directory as `app.ts` (`src`).

``` typescript
export interface Renderable {
    render(): string;
}

export function printRendered(item: Renderable): void {
    console.log(item.render());
}
```

### Defining a few classes that implement Renderable

Back in `app.js` we can now implement renderable in a few classes that we can define.

First, we need to import `Renderer` and `printRendered` at the top of `app.ts` in order to use them in the file.

``` typescript
import { Renderable, printRendered } from './lib'
```

Now we can define a few classes that implement this the `Renderable` interface.

``` typescript
class Person implements Renderable {
  // Constructor to initialize properties
  constructor(public name: string, public age: number) {}

  // Implementation of the render method from the Renderable interface
  render(): string {
      return `Name: ${this.name}, Age: ${this.age}`;
  }
}

enum VehicleType {
  Car = "Car",
  Truck = "Truck",
  Motorcycle = "Motorcycle",
  Bicycle = "Bicycle"
}

class Vehicle {
  constructor(
      public type: VehicleType,
      public make: string,
      public model: string,
      public year: number,
      public mileage?: number,
      public isElectric?: boolean
  ) {}

  render(): string {
      let details = `Vehicle -- Type: ${this.type}, Make: ${this.make}, Model: ${this.model}, Year: ${this.year}`;
      if (this.mileage !== undefined) {
          details += `, Mileage: ${this.mileage} km`;
      }
      if (this.isElectric !== undefined) {
          details += `, Electric: ${this.isElectric ? 'Yes' : 'No'}`;
      }
      return details;
  }
}
```

And instantiate some instances for these classes we just defined:

``` typescript
let person = new Person("John Doe", 30);

const car = new Vehicle(
  VehicleType.Car,
  "Toyota",
  "Camry",
  2022,
  15000, 
  false
);

const truck = new Vehicle(
  VehicleType.Truck,
  "Ford",
  "F-150",
  2020,
  45000,
  false   
);

const motorcycle = new Vehicle(
  VehicleType.Motorcycle,
  "Harley-Davidson",
  "Iron 883",
  2019,
  8000,
  false   
);

const bicycle = new Vehicle(
  VehicleType.Bicycle,
  "Trek",
  "Marlin 5",
  2023,
  undefined,  // Optional Mileage not provided
  true
);

printRendered(bicycle)
```

Check the browser console in developer tools to ensure that bicycle is logged correctly.

Finally, let's make an array of all our objects and render all of them:

``` typescript
const renderableArray = [ person, car, truck, motorcycle, bicycle];
console.log("Rendering each object!")
renderableArray.forEach(renderable => {
  printRendered(renderable)
});
```

### Rendering Components

Let's modify the render functions so they return actual HTML

``` typescript
  // render function for Person
  render(): string {
    return `<div class="person">
              <h2>Person Details</h2>
              <p>Name: ${this.name}</p>
              <p>Age: ${this.age}</p>
            </div>`;
  }

  // render function for Vehicle
  render(): string {
    let details = `<div class="vehicle">
                     <h2>Vehicle Details</h2>
                     <p>Type: ${this.type}</p>
                     <p>Make: ${this.make}</p>
                     <p>Model: ${this.model}</p>
                     <p>Year: ${this.year}</p>`;
    if (this.mileage !== undefined) {
        details += `<p>Mileage: ${this.mileage} km</p>`;
    }
    if (this.isElectric !== undefined) {
        details += `<p>Electric: ${this.isElectric ? 'Yes' : 'No'}</p>`;
    }
    details += `</div>`;
    return details;
  }
```

Now add a function to `lib.js`

``` typescript
export function appendRenderedToBody(item: Renderable): void {
    const outputHtml = item.render();
    document.body.innerHTML += outputHtml; 
}
```

Change the import line in `app.js` to this:

``` typescript
import { Renderable, appendRenderedToBody } from './lib'
```

And change the for-loop to this:

``` typescript
renderableArray.forEach(renderable => {
  appendRenderedToBody(renderable)
});
```

You should now be able to view all the components rendered in the browser!

If not, please review the post.

Now, there was a warning about XSS attacks at the beginning of this series, and `appendRenderedToBody` is currently vulnerable to that. We should fix it.

Install `dompurify`, a library that will sanitize HTML elements: https://www.npmjs.com/package/dompurify.

``` bash
npm install dompurify
```

Rerun `npm run start` and change `appendRenderedToBody` to this:

``` typescript
export function appendRenderedToBody(item: Renderable): void {
  let outputHtml = item.render();
  outputHtml = DOMPurify.sanitize(outputHtml);  // Sanitize the HTML to ensure it's safe to render
  const container = document.createElement('div');
  container.innerHTML = outputHtml;  // Using innerHTML after sanitization to render HTML
  document.body.appendChild(container);
}
```

This should greatly mitigate the risk of XSS attacks.

### Conclusion

As we wrap up this segment of our blog series on building a frontend component framework with TypeScript, we've successfully implemented basic rendering capabilities for our components using object-oriented programming techniques. Starting with defining simple interfaces and classes, we've seen how TypeScript's type system helps in structuring code that is both easy to manage and extend. The practical application of these concepts was demonstrated by rendering components directly into the browser, showcasing the immediate visual feedback that's crucial for frontend development.

We've also addressed a significant security concern—cross-site scripting (XSS)—by integrating DOMPurify to sanitize our output, ensuring that our framework not only functions effectively but also securely. This step is crucial as we prepare our components to handle dynamic and potentially untrusted data.

The journey from basic TypeScript setups to rendering HTML components in the browser offers a glimpse into the power of TypeScript in building scalable and maintainable web applications. As we continue to explore more advanced topics in future posts, keep experimenting with the codebase and leverage these foundational skills to enhance your projects.

You can continue exploring the evolution of our TypeScript framework and perhaps integrate more complex features or optimizations on your own. The current codebase up to this point is available on GitHub at Simple TS Framework on the branch `render-basic-components` at https://github.com/tim-br/simple-ts-framework/tree/render-basic-components.