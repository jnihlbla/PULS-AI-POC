000100 01  REQU-W40327I1.                                                       
000200*                                 REQUEST TO PGM  W40327                  
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-KDMATT          PIC X.                                       
000600*                                 MÅTTKOD                                 
000700     03 REQU-IDPRC-KEY.                                                   
000800*                                 PRODUKTIONSKANAL                        
000900        05 REQU-IDPRCBAS     PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 REQU-IDPRCVAR     PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 REQU-IDLOTNR-KEY     PIC 9(3).                                    
001400*                                 VAGN-NUMMER                             
001500     03 REQU-KVRADER-MAX1    PIC 9(5).                                    
001600*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001700*                                 EDAN.                                   
001800     03 REQU-FLSKRIV-CLABEL  PIC X.                                       
001900*                                 J/Y = SKRIV BEGÄRD LISTA                
002000     03 REQU-FLSKRIV-DELNOTE PIC X.                                       
002100*                                 J/Y = SKRIV BEGÄRD LISTA                
002200     03 REQU-PRTVAL-ADRESSFL PIC X(2).                                    
002300*                                 PRINTER-VAL KOD ADRESS FLAGGA           
002400     03 REQU-PRTVAL-FOLJEFL  PIC X(2).                                    
002500*                                 PRINTER-VAL KOD FÖLJESEDEL              
002600     03 REQU-RAD             OCCURS 500 TIMES.                            
002700*                                 MID-COPYTEXT FÖR WL0197                 
002800        05 REQU-FLSKRIV      PIC X.                                       
002900*                                 ALLMÄN FLAGGA                           
003000        05 REQU-IDPRODNR     PIC 9(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200        05 REQU-IDPLKLST     PIC 9(3).                                    
003300*                                 PLOCKLISTNUMMER                         
003400        05 REQU-IDDISTR      PIC 9(4).                                    
003500*                                 DISTRIKTNUMMER                          
003600        05 REQU-IDKUNDNR     PIC 9(6).                                    
003700*                                 KUNDNUMMER                              
003800        05 REQU-IDORDNR      PIC 9(5).                                    
003900*                                 ORDERNUMMER UTGÅR PD90                  
004000        05 REQU-IDKOLLI      PIC 9(5).                                    
004100*                                 KOLLINUMMER                             
004200        05 REQU-IDKOLLI-OLD  PIC 9(5).                                    
004300*                                 KOLLINUMMER                             
004400        05 REQU-KDKOLLI      PIC X(8).                                    
004500*                                 KOLLIKOD                                
004600        05 REQU-VKORDBTO     PIC X(8).                                    
004700*                                 ORDERVIKT BRUTTO (KG)                   
004800        05 REQU-DIKOLLIL     PIC 9(4).                                    
004900*                                 KOLLI-LÄNGD                             
005000        05 REQU-DIKOLLIH     PIC 9(3).                                    
005100*                                 KOLLI-HÖJD                              
005200        05 REQU-DIKOLLIB     PIC 9(3).                                    
005300*                                 KOLLI-BREDD                             
005400*** END OF VILMAII-COPY LENGTH= 31021 BYTES                               
