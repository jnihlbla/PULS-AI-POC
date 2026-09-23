000100 01  W27179-CTX.                                                          
000200*                                 COPYTEXT TILL FILEN W27179,             
000300*                                 UPPDATERA IDPERSON-BUY                  
000400*                                           KDREFTYP                      
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001000*                                 PERSONKOD REFILLANSVARIG                
001100     03 IDREFTAB             PIC X.                                       
001200*                                 IDENTITET REFILLTABELL                  
001300     03 FLBUYUPD             PIC X.                                       
001400*                                 OM IDPERSONKOD ÄR LÅST                  
001500     03 FLTABUPD             PIC X.                                       
001600*                                 OM REFILLTABELL ÄR LÅST                 
001700     03 KDDCSTYR-BUY         PIC S9(5)           COMP-3.                  
001800*                                 REGELVERK VID BUYERTILLDELNING          
001900*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
