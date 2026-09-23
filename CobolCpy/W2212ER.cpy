000100 01  W2212ER.                                                             
000200*                                 FELLISTA PÅ 220-LARM ARTIKLAR           
000300*                                 SOM EJ SKICKAT MAIL TILL                
000400*                                 LEVERANTÖREN                            
000500*                                 ERROR LIST ON PARTS THAT HAS            
000600*                                 NOT SENT MAIL TO THE SUPPLIER           
000700*                                 WITH ALERT CODE 220                     
000800     03 IDARTNR              PIC 9(8).                                    
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 KDMAIL               PIC X(4).                                    
001800*                                 TYP AV MAIL UTSKICK                     
001900*                                 TYPE OF MAIL SENDNINGS                  
002000     03 IDATTENT             PIC 9(2).                                    
002100*                                 ATTENTION NUMMER                        
002200*                                 ATTENTION NUMBER                        
002300     03 IDANSK               PIC 9(3).                                    
002400*                                 ANSKAFFARNUMMER                         
002500*                                 PROCURER NO.                            
002600     03 IDNAMN-ANSK          PIC X(40).                                   
002700*                                 NAMN                                    
002800     03 TENOTE               PIC X(40).                                   
002900*                                 NOTERINGSFÄLT                           
003000*                                 NOTE FIELD                              
003100*** END OF VILMAII-COPY LENGTH= 104 BYTES                                 
