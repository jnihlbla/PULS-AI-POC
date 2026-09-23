000100 01  REQU-WL0199I1.                                                       
000200*                                 REQUEST TO PGM  WL0199                  
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDANSTNR-KEY    PIC 9(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 REQU-IDPRODNR-KEY    PIC 9(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 REQU-IDPLKLST-KEY    PIC 9(3).                                    
001000*                                 PLOCKLISTNUMMER                         
001100     03 REQU-KVRADER-MAX1    PIC 9(5).                                    
001200*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001300*                                 EDAN.                                   
001400     03 REQU-FLSISTAK        PIC X.                                       
001500*                                 SISTA KOLLI I ORDERN?                   
001600     03 REQU-FLSKRIV-CLABEL  PIC X.                                       
001700*                                 J/Y = SKRIV BEGÄRD LISTA                
001800     03 REQU-FLSKRIV-DELNOTE PIC X.                                       
001900*                                 J/Y = SKRIV BEGÄRD LISTA                
002000     03 REQU-RESTART-KVKOLLI PIC 9(4).                                    
002100*                                 ANTAL KOLLI                             
002200     03 REQU-RESTART-VKORDNTO                                             
002300                             PIC 9(6)V9(1).                               
002400*                                 ORDERVIKT NETTO (KG)                    
002500     03 REQU-RESTART-VKORDBTO                                             
002600                             PIC 9(6)V9(1).                               
002700*                                 ORDERVIKT BRUTTO (KG)                   
002800     03 REQU-RESTART-VLORDBTO                                             
002900                             PIC 9(4)V9(3).                               
003000*                                 ORDERVOLYM BRUTTO (M3)                  
003100     03 REQU-RESTART-SUORDV  PIC 9(9)V9(2).                               
003200*                                 SUMMA ORDERVÄRDE                        
003300     03 REQU-RESTART-SUORDV-LOC                                           
003400                             PIC 9(9)V9(2).                               
003500*                                 SUMMA ORDERVÄRDE                        
003600     03 REQU-RESTART-SUORDV-LOCPREL                                       
003700                             PIC 9(9)V9(2).                               
003800*                                 SUMMA ORDERVÄRDE                        
003900     03 REQU-RESTART-IDRADNR PIC 9(4).                                    
004000*                                 RADNUMMER                               
004100     03 REQU-RESTART-IDKOLLI PIC 9(5).                                    
004200*                                 KOLLINUMMER                             
004300     03 REQU-RESTART-KDKOLLI PIC X(8).                                    
004400*                                 KOLLIKOD                                
004500     03 REQU-KDMATT          PIC X.                                       
004600*                                 MÅTTKOD                                 
004700     03 REQU-PRTVAL-ADRESSFL PIC X(2).                                    
004800*                                 PRINTER-VAL KOD ADRESS FLAGGA           
004900     03 REQU-PRTVAL-FOLJEFL  PIC X(2).                                    
005000*                                 PRINTER-VAL KOD FÖLJESEDEL              
005100     03 REQU-RAD             OCCURS 1 TO 100 TIMES                        
005200                             DEPENDING ON REQU-KVRADER-MAX1.              
005300*                                 MID-COPYTEXT FÖR WL0199                 
005400        05 REQU-IDKOLLI      PIC 9(5).                                    
005500*                                 KOLLINUMMER                             
005600        05 REQU-KDKOLLI      PIC X(8).                                    
005700*                                 KOLLIKOD                                
005800        05 REQU-IDRADNR-FOM  PIC 9(4).                                    
005900*                                 RADNUMMER                               
006000        05 REQU-IDRADNR-TOM  PIC 9(4).                                    
006100*                                 RADNUMMER                               
006200        05 REQU-KVLEVART     PIC 9(7).                                    
006300*                                 LEVERERAT ANTAL STYCK                   
006400        05 REQU-KDEMBTYP     PIC 9.                                       
006500*                                 EMBALLAGETYP                            
006600        05 REQU-DIKOLLIL     PIC 9(4).                                    
006700*                                 KOLLI-LÄNGD                             
006800        05 REQU-DIKOLLIB     PIC 9(3).                                    
006900*                                 KOLLI-BREDD                             
007000        05 REQU-DIKOLLIH     PIC 9(3).                                    
007100*                                 KOLLI-HÖJD                              
007200        05 REQU-KDARTURS     PIC X(2).                                    
007300*                                 ARTIKELURSPRUNGSKOD                     
007400*** END OF VILMAII-COPY LENGTH= 4205 BYTES                                
