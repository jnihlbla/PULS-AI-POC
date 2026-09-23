000100 01  W27113-CTX.                                                          
000200*                                 COPYTEXT TILL FILEN W27113              
000300*                                 RENSNINGSFIL FÖR IVÄG-                  
000400*                                 SKICKADE REFILLORDER SAMT               
000500*                                 RETURORDER                              
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
000900*                                 PERSONKOD REFILLANSVARIG                
001000     03 KDREFTYP             PIC X.                                       
001100*                                 TYP AV REFILLORDER                      
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IDDISTR              PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600     03 FLREFNYO             PIC X.                                       
001700*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
001800*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
