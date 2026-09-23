000100 01  W2711G-CTX.                                                          
000200*                                 COPYTEXT FÖR FILEN W2711G               
000300*                                 EXTRAKTFIL AV                           
000400*                                 REFILLFÖRSLAG                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
000800*                                 PERSONKOD REFILLANSVARIG                
000900     03 KDREFTYP             PIC X.                                       
001000*                                 TYP AV REFILLORDER                      
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 KDREFORS             PIC X.                                       
001400*                                 REFILL ORDER STATUSKOD                  
001500     03 KVBEART              PIC S9(7)           COMP-3.                  
001600*                                 BESTÄLLT ANTAL STYCKEN                  
001700     03 TEREFMED             PIC X(75).                                   
001800*                                 INFOMEDDELANDE REFILLARTIKEL            
001900*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
