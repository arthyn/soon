import { TailwindProvider } from 'tailwind-rn';
import utilities from './tailwind.json';
import { Main } from './src/Main';

export default function App() {
  return (
    <TailwindProvider utilities={utilities}>
      <Main />
    </TailwindProvider>
  );
}
