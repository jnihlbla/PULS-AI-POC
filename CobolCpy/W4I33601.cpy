000100 01  MID-W4I33601.                                                        
000200*                                 MID-COPYTEXT FÖR W4033600               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDORDNR-IN       PIC X(5).                                    
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500     03 MID-IDKOLLI-IN       PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MID-IDKOLLI-UT       PIC X(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-KDPRTVAL-IN      PIC X(2).                                    
002400*                                 PRINTER-VAL KOD                         
002500     03 MID-KDPRTVAL-UT      PIC X(2).                                    
002600*                                 PRINTER-VAL KOD                         
002700     03 MID-IDKOLLI-FLER     PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900     03 MID-IDKOLLI-FIRST    PIC 9(5).                                    
003000*                                 KOLLINUMMER                             
003100     03 MID-IDKOLLI-NEXT     PIC 9(5).                                    
003200*                                 KOLLINUMMER                             
003300     03 MID-IDPRODNR         PIC 9(7).                                    
003400*                                 PRODUKTIONSNUMMER                       
003500     03 MID-KDKOLLI-BAER-IN  PIC X(8).                                    
003600*                                 KOLLIKOD                                
003700     03 MID-KDEMBTYP-BAER-IN PIC X.                                       
003800*                                 EMBALLAGETYP                            
003900     03 MID-DIKOLLIL-BAER-IN PIC X(4).                                    
004000*                                 KOLLI-LÄNGD                             
004100     03 MID-DIKOLLIB-BAER-IN PIC X(3).                                    
004200*                                 KOLLI-BREDD                             
004300     03 MID-DIKOLLIH-BAER-IN PIC X(3).                                    
004400*                                 KOLLI-HÖJD                              
004500     03 MID-W4I33601-001     OCCURS 8 TIMES.                              
004600*                                 MID-COPYTEXT FÖR W4033600               
004700        05 MID-IDKOLLI-ING-UT                                             
004800                             PIC X(5).                                    
004900*                                 KOLLINUMMER                             
005000     03 MID-KDCMD            PIC X.                                       
005100      88 MID-KDCMD-INGENTING VALUE ' '.                                   
005200      88 MID-KDCMD-DELETE    VALUE 'D'                                    
005300                             'B'.                                         
005400      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
005500                             'Ä'.                                         
005600      88 MID-KDCMD-INSERT    VALUE 'I'                                    
005700                             'N'.                                         
005800*                                 RAD-UPPDATERINGSKOMMANDO                
005900*                                  BLANK  = INGENTING                     
006000*                                  D , B  = DELETE                        
006100*                                  R , Ä  = REPLACE                       
006200*                                  I , N  = INSERT                        
006300     03 MID-IDKOLLI-ING-NY   PIC X(5).                                    
006400*                                 KOLLINUMMER                             
006500     03 MID-VKORDBTO-ING-NY  PIC X(8).                                    
006600*                                 ORDERVIKT BRUTTO (KG)                   
006700     03 MID-KDKOLLI-ING-NY   PIC X(8).                                    
006800*                                 KOLLIKOD                                
006900     03 MID-KDEMBTYP-ING-NY  PIC X.                                       
007000*                                 EMBALLAGETYP                            
007100     03 MID-DIKOLLIL-ING-NY  PIC X(4).                                    
007200*                                 KOLLI-LÄNGD                             
007300     03 MID-DIKOLLIB-ING-NY  PIC X(3).                                    
007400*                                 KOLLI-BREDD                             
007500     03 MID-DIKOLLIH-ING-NY  PIC X(3).                                    
007600*                                 KOLLI-HÖJD                              
007700     03 MID-ADFLGEO          PIC X(3).                                    
007800*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
007900     03 MID-ADFLOMR          PIC X(3).                                    
008000*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
008100     03 MID-ADRUTNIV         PIC X(3).                                    
008200*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
008300     03 MID-ADVMODUL         PIC X(3).                                    
008400*                                 VÄNSTER-MODUL                           
008500     03 MID-ADHMODUL         PIC 9(3).                                    
008600*                                 HÖGER-MODUL                             
008700     03 MID-DIDMODUL         PIC 9(3).                                    
008800*                                 MODUL-DJUP                              
008900     03 MID-DIHMODUL         PIC 9(3).                                    
009000*                                 MODUL-HÖJD                              
009100     03 MID-IDTRPTNR         PIC 9(3).                                    
009200*                                 TRANSPORTIDENTITET                      
009300     03 MID-FLUTLAST         PIC X.                                       
009400*                                 KOLLI I UTLASTNINGSLAGER                
009500*** END OF VILMAII-COPY LENGTH= 187 BYTES                                 
