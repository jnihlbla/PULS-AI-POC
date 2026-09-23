000100 01  W513473.                                                             
000200*                                 COPYTEXT TILL MANAGEMNENT NEG.          
000300*                                 BALANCE REPORT LDC                      
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDARTNR              PIC Z(7)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 BEART                PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 KVAKS                PIC -(7)9.                                   
001300*                                 ANKOMSTSALDO                            
001400     03 KVEFRS               PIC -(7)9.                                   
001500*                                 EJ FAKTURERAT ANTAL STYCK               
001600     03 KVLS                 PIC -(7)9.                                   
001700*                                 LAGERSALDO                              
001800     03 DAAAVV               PIC 9(6).                                    
001900*                                 ÅR - VECKA  (ÅÅÅÅVV)                    
002000     03 ADCITY               PIC X(20).                                   
002100     03 IDLANDX2             PIC X(2).                                    
002200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002300*** END OF VILMAII-COPY LENGTH= 97 BYTES                                  
