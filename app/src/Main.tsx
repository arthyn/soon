import { StatusBar } from "expo-status-bar";
import { Text, View } from "react-native";
import { useTailwind } from "tailwind-rn";

export const Main = () => {
  const tailwind = useTailwind();
  return (
    <View style={tailwind('flex h-full w-full items-center justify-center font-semibold bg-green-100')}>
      <Text>Open up App.tsx to start working on your app!</Text>
      <StatusBar style="auto" />
    </View>
  )
}