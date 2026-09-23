000100 01  W418AU.                                                              
000200*                                 FAKTURA-FIL RETUR AV RETUR              
000300*                                                                         
000400     03 IDHTYP               PIC X(4).                                    
000500*                                 HÄNDELSETYP                             
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 IDRAPP               PIC X(10).                                   
001300*                                 RAPPORT ID                              
001400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001500*                                 KOLLINUMMER                             
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 DAREGDAT             PIC 9(8).                                    
001900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002000     03 DARFSDAT             PIC 9(8).                                    
002100*                                 KLART FÖR TRANSPORT ÅÅÅÅMMDD            
002200     03 IDAVS                PIC X(20).                                   
002300*                                 IDENTITET PÅ DEN PERSON SOM             
002400*                                 SKICKAT IVÄG GODS                       
002500     03 KDFEL                PIC S9(3)           COMP-3.                  
002600*                                 FELKOD                                  
002700     03 KVANTAL              PIC S9(7)           COMP-3.                  
002800*                                 ANTAL                                   
002900     03 TENOTE               PIC X(40).                                   
003000*                                 NOTERINGSFÄLT                           
003100*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
