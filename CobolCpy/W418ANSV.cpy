000100 01  ANSV-W418ANSV.                                                       
000200*                                 LÄNKAREA TILL W418ANSV -                
000300*                                 KONTROLL AV ANSVARIG.                   
000400*                                 INAREA:                                 
000500*                                   HELA INAREAN MÅSTE FYLLAS I.          
000600*                                   KDCALL ANGER VILKEN ANSVARIG          
000700*                                   SOM SÖKES:                            
000800*                                     1=ANSVARIG LEV.ANM.AVD.             
000900*                                     2=ANSVARIG REMISS                   
001000*                                     3=ANSVARIG RETURAVD.                
001100*                                 UTAREA:                                 
001200*                                   SVAR FÅS MED:                         
001300*                                   " " = OK, ANSVARIG FUNNEN             
001400*                                   "S" = INTERVALL SAKNAS                
001500*                                   "F" = FEL KDCALL EL. DISTR=0.         
001600     03 ANSV-INDATA.                                                      
001700*                                 INDATA TILL W418ANSV                    
001800        05 ANSV-KDCALL       PIC 9(3).                                    
001900*                                 ANROPSTYP                               
002000        05 ANSV-IDDISTR      PIC 9(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200        05 ANSV-IDDC         PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400        05 ANSV-IDFTG        PIC 9(2).                                    
002500*                                 FÖRETAGSID EKONOM REDOVISNING           
002600        05 ANSV-IDKUNDNR     PIC 9(6).                                    
002700*                                 KUNDNUMMER                              
002800        05 ANSV-KDANMORS     PIC X(2).                                    
002900*                                 ORSAK TILL LEVERANSANMÄRKNING           
003000        05 ANSV-KDORDKL      PIC 9.                                       
003100*                                 ORDERKLASS                              
003200        05 ANSV-ADLAGOMR     PIC 9(2).                                    
003300*                                 LAGEROMRÅDE                             
003400     03 ANSV-UTDATA.                                                      
003500*                                 UTDATA TILL W418ANSV                    
003600        05 ANSV-KDSVAR       PIC X.                                       
003700         88 ANSV-OK          VALUE ' '.                                   
003800         88 ANSV-SAKNAS      VALUE 'S'.                                   
003900         88 ANSV-FEL         VALUE 'F'.                                   
004000*                                                       KDSVAR-88         
004100*                                 SVARSKOD FRÅN SUBPROGRAM                
004200        05 ANSV-KDARBTYP     PIC X(8).                                    
004300*                                 TYP AV ARBETE                           
004400        05 ANSV-IDPERSON     PIC 9(3).                                    
004500*                                 PERSONKOD                               
004600*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
