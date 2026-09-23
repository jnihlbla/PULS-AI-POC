000100 01  MID-W4I74501.                                                        
000200*                                 MID-COPYTEXT FÖR W40745                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDRAPP-IN        PIC X(10).                                   
000800*                                 RAPPORT ID                              
000900     03 MID-IDKOLLI-IN       PIC X(5).                                    
001000*                                 KOLLINUMMER                             
001100     03 MID-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MID-INPUT.                                                        
001400*                                 INMATNINGSFÄLT                          
001500        05 MID-IDAVS-UPD     PIC X(20).                                   
001600*                                 IDENTITET PÅ DEN PERSON SOM             
001700*                                 SKICKAT IVÄG GODS                       
001800        05 MID-IDKOLLI-UPD   PIC 9(5).                                    
001900*                                 KOLLINUMMER                             
002000        05 MID-KDKOLLI-UPD   PIC X(8).                                    
002100*                                 KOLLIKOD                                
002200        05 MID-VKORDBTO-KOLLI-UPD                                         
002300                             PIC X(8).                                    
002400*                                 ORDERVIKT BRUTTO PER KOLLI              
002500        05 MID-FLTABORT      PIC X.                                       
002600*                                 BORTTAGSFLAGGA                          
002700        05 MID-FLKLAR        PIC X.                                       
002800*                                 AVSLUTNINGSMARKERING                    
002900        05 MID-KDPRTVAL      PIC X.                                       
003000*                                 PRINTER-VAL KOD                         
003100        05 MID-INPUT-RADER   OCCURS 10 TIMES.                             
003200*                                 NYCKELFÄLT PÅ RADEN                     
003300           07 MID-KDCMD      PIC X.                                       
003400            88 MID-KDCMD-INGENTING                                        
003500                             VALUE ' '.                                   
003600            88 MID-KDCMD-DELETE                                           
003700                             VALUE 'D'                                    
003800                             'B'.                                         
003900            88 MID-KDCMD-REPLACE                                          
004000                             VALUE 'R'                                    
004100                             'Ä'.                                         
004200            88 MID-KDCMD-INSERT                                           
004300                             VALUE 'I'                                    
004400                             'N'.                                         
004500            88 MID-KDCMD-SELECT                                           
004600                             VALUE 'S'                                    
004700                             'V'.                                         
004800            88 MID-KDCMD-PRINT                                            
004900                             VALUE 'P'                                    
005000                             'P'.                                         
005100*                                 RAD-UPPDATERINGSKOMMANDO                
005200*                                  BLANK  = INGENTING                     
005300*                                  D , B  = DELETE                        
005400*                                  R , Ä  = REPLACE                       
005500*                                  I , N  = INSERT                        
005600*                                  S , V  = SELECT                        
005700*                                  P , P  = PRINT                         
005800           07 MID-IDARTNR    PIC 9(8).                                    
005900*                                 ARTIKELNUMMER                           
006000           07 MID-KVANTAL    PIC 9(6).                                    
006100*                                 ANTAL                                   
006200           07 MID-PRARTBTO   PIC X(10).                                   
006300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
006400           07 MID-KDFEL      PIC 9(3).                                    
006500*                                 FELKOD                                  
006600           07 MID-TENOTE     PIC X(40).                                   
006700*                                 NOTERINGSFÄLT                           
006800*** END OF VILMAII-COPY LENGTH= 751 BYTES                                 
