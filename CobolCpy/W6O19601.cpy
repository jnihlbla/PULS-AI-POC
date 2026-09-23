000100 01  MOD-W6O19601.                                                        
000200*                                 MODCOPYTEXT TILL W601109                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300*                                 SUPPLIER NUMBER                         
001400     03 MOD-IDOKOLLI-IN-ATTR PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
001700*                                 ODETTE KOLLINUMMER                      
001800*                                 ODETTE CASE NUMBER                      
001900     03 MOD-IDLOPNRM-IN-ATTR PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDLOPNRM-IN      PIC Z(8)9.                                   
002200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002300*                                 (0VVDLLLLK)                             
002400*                                 SERIAL NO RECEIVING REPORT              
002500*                                 (0WWDLLLLC)                             
002600     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-IDDC-IN          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100     03 MOD-ADINLOMR-PRT-ATTR                                             
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
003500*                                 PRINTERPLACERING                        
003600*                                 PLACE OF A PRINTER                      
003700     03 MOD-TEMFSINF         PIC X(55).                                   
003800*                                 INFORMATIONSMEDDELANDE                  
003900*                                 INFORMATION MESSAGE                     
