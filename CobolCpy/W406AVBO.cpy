000100 01  AVBOK-W406BOKA.                                                      
000200*                                 PARAMETERAREA TILL AVBOKNING /          
000300*                                     BORTTAG AV SPÄRR                    
000400*                                    ++++++++++++++++++                   
000500*                                 PLATSSPÄRR BORTTAS I RUTA ELLER         
000600*                                 STÄLLAGE, SPÄRR SLÄPPS                  
000700*                                 ----- INDATA ------------------         
000800*                                 UPPGIFTER SOM ALLTID SKALL              
000900*                                 VARA IFYLLDA:                           
001000*                                                                         
001100*                                 KDCALL: 3 ---> AVBOKA                   
001200*                                         5 ---> SLÄPP SPÄRR              
001300*                                                                         
001400*                                 IDTRPTNR + ADCLGEO + ADFLOMR +          
001500*                                 ADRUTNIV + ADVMODUL + ADHMODUL          
001600*                                 + DIHMODUL + DIDMODUL                   
001700*                                                                         
001800*                                                                         
001900*                                 ----- SVAR   ------------------         
002000*                                                                         
002100*                                 KDSVAR  : BLANK --> OK                  
002200*                                           F     --> EJ OK               
002300     03 AVBOK-KDCALL         PIC S9(3)           COMP-3.                  
002400*                                 ANROPSTYP                               
002500     03 AVBOK-IDTRPTNR       PIC S9(3)           COMP-3.                  
002600*                                 TRANSPORTIDENTITET                      
002700     03 AVBOK-TIRFS          PIC S9(11)          COMP-3.                  
002800*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
002900     03 AVBOK-ADCLGEO.                                                    
003000*                                 IDDC + GEO ADRESS                       
003100        05 AVBOK-IDDC        PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300        05 AVBOK-ADFLGEO     PIC X(3).                                    
003400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
003500     03 AVBOK-ADFLOMR        PIC S9(3)           COMP-3.                  
003600*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003700     03 AVBOK-ADRUTNIV       PIC S9(3)           COMP-3.                  
003800*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003900     03 AVBOK-DIHMODUL       PIC S9(3)           COMP-3.                  
004000*                                 MODUL-HÖJD                              
004100     03 AVBOK-DIDMODUL       PIC S9(3)           COMP-3.                  
004200*                                 MODUL-DJUP                              
004300     03 AVBOK-ADVMODUL       PIC S9(3)           COMP-3.                  
004400*                                 VÄNSTER-MODUL                           
004500     03 AVBOK-ADHMODUL       PIC S9(3)           COMP-3.                  
004600*                                 HÖGER-MODUL                             
004700     03 AVBOK-KDSVAR         PIC X.                                       
004800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
004900*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
