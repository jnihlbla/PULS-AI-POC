000100 01  W430432.                                                             
000200*                                 POSTBESKRIVNING FÖR                     
000300*                                 W43022-FILEN                            
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 IDDISTR              PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 KDORDKL              PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 TIAVBAR              PIC S9(3)           COMP-3.                  
001600*                                 AVBOKNINGSÅR (ÅÅ)                       
001700     03 TIAVBPER             PIC S9              COMP-3.                  
001800*                                 AVBOKNINGSPERIOD                        
001900     03 KVBEART              PIC S9(7)           COMP-3.                  
002000*                                 BESTÄLLT ANTAL STYCKEN                  
002100     03 KDQPACK              PIC S9              COMP-3.                  
002200*                                 TYP AV KVANT-FÖRPACKNING                
002300*** END COPY W430432     LENGTH=22                                        
