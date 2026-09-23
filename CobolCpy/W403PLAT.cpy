000100 01  PLATS-W403PLAT.                                                      
000200*                                 PARAMETERAREA TILL                      
000300*                                                    PLATSBOKNIG,         
000400*                                                   PLATSSÖKNING,         
000500*                                                 PLATSAVBOKNING          
000600*                                 +++++++++++++++++++++++++++++++         
000700*                                          BOKNING:                       
000800*                                 +++++++++++++++++++++++++++++++         
000900*                                 PLATS RESERVERAS I RUTA ELLER           
001000*                                 STÄLL, ALT NY PLATS SKAPAS              
001100*                                                                         
001200*                                     - INDATA -                          
001300*                                 FYLL I:                                 
001400*                                 KDCALL: 4 ---> BOKA                     
001500*                                                                         
001600*                                 FYLL I OM RUT-ADRESS:                   
001700*                                 ADCLGEO + ADFLOMR + ADRUTNIV            
001800*                                 NOLL I: DIHMODUL + DIDMODUL +           
001900*                                 ADVMODUL + ADHMODUL                     
002000*                                                                         
002100*                                 FYLL I OM STÄLL-ADRESS:                 
002200*                                 ADCLGEO + ADFLOMR + ADRUTNIV +          
002300*                                 DIHMODUL + DIDMODUL +                   
002400*                                 ADVMODUL + ADHMODUL                     
002500*                                                                         
002600*                                 FYLL I ALT NOLL/SPACE-UTFYLL:           
002700*                                 VLRUTNIV + TESPAERR                     
002800*                                                                         
002900*                                     - UTDATA -                          
003000*                                 KDSVAR  : BLANK --> OK                  
003100*                                           F     --> EJ OK               
003200*                                                                         
003300*                                 +++++++++++++++++++++++++++++++         
003400*                                          SÖKNING:                       
003500*                                 +++++++++++++++++++++++++++++++         
003600*                                 PLATS SÖKS I RUTA ELLER STÄLL,          
003700*                                 ALT. ANGIVEN PLATS KONTROLLERAS         
003800*                                                                         
003900*                                     - INDATA -                          
004000*                                 SÖKNING:                                
004100*                                 KDCALL: 0 ---> STÄLLAGE SÖKS            
004200*                                         1 ---> BULKORDER SLASK          
004300*                                         2 ---> DAGORDER  SLASK          
004400*                                         6 ---> FARLIGT GODS             
004500*                                 SAMTLIGA KUND- OCH KOLLI-               
004600*                                 UPPGIFTER SAMT KDCLAGER                 
004700*                                 IFYLLS ALLTID                           
004800*                                 ADFLGEO + ADFLOMR + ADRUTNIV:           
004900*                                 IFYLLS OM GIVEN ADRESS SKALL            
005000*                                 KONTROLLERAS; ANNARS --> NOLL           
005100*                                                                         
005200*                                 IDTRPTNR +                              
005300*                                 ADVMODUL + ADHMODUL: --> NOLL           
005400*                                                                         
005500*                                 FLUTLAST:            --> SPACE          
005600*                                                                         
005700*                                     - UTDATA -                          
005800*                                 IDTRPTNR + ADCLGEO + ADFLOMR +          
005900*                                 ADRUTNIV + ADVMODUL + ADHMODUL          
006000*                                 + FLUTLAST                              
006100*                                                                         
006200*                                 KDSVAR  : BLANK --> OK                  
006300*                                           F     --> EJ OK               
006400*                                                                         
006500*                                 +++++++++++++++++++++++++++++++         
006600*                                          AVBOKNING:                     
006700*                                 +++++++++++++++++++++++++++++++         
006800*                                 PLATSSPÄRR BORTTAS I RUTA ELLER         
006900*                                 STÄLLAGE, SPÄRR SLÄPPS                  
007000*                                                                         
007100*                                     - INDATA -                          
007200*                                 UPPGIFTER SOM ALLTID SKALL              
007300*                                 VARA IFYLLDA:                           
007400*                                                                         
007500*                                 KDCALL: 3 ---> AVBOKA                   
007600*                                         5 ---> SLÄPP SPÄRR              
007700*                                                                         
007800*                                 IDTRPTNR + ADCLGEO + ADFLOMR +          
007900*                                 ADRUTNIV + ADVMODUL + ADHMODUL          
008000*                                 + DIHMODUL + DIDMODUL                   
008100*                                                                         
008200*                                     - UTDATA -                          
008300*                                                                         
008400*                                 KDSVAR  : BLANK --> OK                  
008500*                                           F     --> EJ OK               
008600*                                                                         
008700     03 PLATS-KDCALL         PIC S9(3)           COMP-3.                  
008800*                                 ANROPSTYP                               
008900     03 PLATS-IDDISTR        PIC S9(5)           COMP-3.                  
009000*                                 DISTRIKTNUMMER                          
009100     03 PLATS-IDKUNDNR       PIC S9(7)           COMP-3.                  
009200*                                 KUNDNUMMER                              
009300     03 PLATS-KDFRAKT        PIC S9(3)           COMP-3.                  
009400*                                 FRAKTSÄTT DC TILL KUND                  
009500     03 PLATS-KDORDKLX       PIC X.                                       
009600*                                 ORDERKLASS + BLANK                      
009700     03 PLATS-IDORDNR        PIC S9(5)           COMP-3.                  
009800*                                 ORDERNUMMER UTGÅR PD90                  
009900     03 PLATS-DIKOLLIH       PIC S9(3)           COMP-3.                  
010000*                                 KOLLI-HÖJD                              
010100     03 PLATS-DIKOLLIB       PIC S9(3)           COMP-3.                  
010200*                                 KOLLI-BREDD                             
010300     03 PLATS-DIKOLLIL       PIC S9(5)           COMP-3.                  
010400*                                 KOLLI-LÄNGD                             
010500     03 PLATS-KDKOLLID       PIC X.                                       
010600*                                 KOLLI-DJUP                              
010700     03 PLATS-VKORDNTO-KOLLI PIC S9(6)V9(1)      COMP-3.                  
010800*                                 ORDERVIKT NETTO PER KOLLI               
010900     03 PLATS-IDTRPTNR       PIC S9(3)           COMP-3.                  
011000*                                 TRANSPORTIDENTITET                      
011100     03 PLATS-ADCLGEO.                                                    
011200*                                 IDDC + GEO ADRESS                       
011300        05 PLATS-IDDC        PIC X(2).                                    
011400*                                 IDENTIFIERARE LAGER                     
011500        05 PLATS-ADFLGEO     PIC X(3).                                    
011600*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
011700     03 PLATS-ADFLOMR        PIC S9(3)           COMP-3.                  
011800*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
011900     03 PLATS-ADRUTNIV       PIC S9(3)           COMP-3.                  
012000*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
012100     03 PLATS-DIHMODUL       PIC S9(3)           COMP-3.                  
012200*                                 MODUL-HÖJD                              
012300     03 PLATS-DIDMODUL       PIC S9(3)           COMP-3.                  
012400*                                 MODUL-DJUP                              
012500     03 PLATS-ADVMODUL       PIC S9(3)           COMP-3.                  
012600*                                 VÄNSTER-MODUL                           
012700     03 PLATS-ADHMODUL       PIC S9(3)           COMP-3.                  
012800*                                 HÖGER-MODUL                             
012900     03 PLATS-FLUTLAST       PIC X.                                       
013000*                                 KOLLI I UTLASTNINGSLAGER                
013100     03 PLATS-TESPAERR       PIC X(20).                                   
013200*                                 SPÄRRTEXT                               
013300     03 PLATS-VLRUTNIV       PIC S9(3)           COMP-3.                  
013400*                                 TOT KOLLI-VOLYM I RUTA/NIV (M3)         
013500     03 PLATS-TIRFS          PIC S9(11)          COMP-3.                  
013600*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
013700     03 PLATS-IDDC-CROSS     PIC X(2).                                    
013800*                                 DC FÖR CROSS DOCKING                    
013900     03 PLATS-KDSVAR         PIC X.                                       
014000*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
014100*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
