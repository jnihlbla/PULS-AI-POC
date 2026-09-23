000100 01  MID-W1I54201.                                                        
000200*                                 MID-COPYTEXTEN FÖR BILD                 
000300*                                 ARTIKELFRÅGA MASTER                     
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-KDFORDON-IN      PIC X(2).                                    
000900*                                 FORDONSSLAG                             
001000     03 MID-KDFORDON-UT      PIC X(2).                                    
001100*                                 FORDONSSLAG                             
001200     03 MID-IDKATNR-IN       PIC X(5).                                    
001300*                                 KATALOG-ID                              
001400     03 MID-IDKATNR-UT       PIC X(5).                                    
001500*                                 KATALOG-ID                              
001600     03 MID-SEGM-NYCKEL.                                                  
001700*                                 DOLD SEGMENT NYCKEL                     
001800        05 MID-IDFORDON      PIC 9(3).                                    
001900*                                 FORDONSSLAG                             
002000        05 MID-TIOMBRYT-9KOMPL                                            
002100                             PIC 9(7).                                    
002200*                                 OMBRYTNINGSDATUM 9-KOMPLEMENT           
002300     03 MID-LINE             OCCURS 13 TIMES.                             
002400*                                 RAD                                     
002500        05 MID-COL           OCCURS 3 TIMES.                              
002600*                                 KOLUMN                                  
002700           07 MID-KDCMD      PIC X.                                       
002800            88 MID-KDCMD-INGENTING                                        
002900                             VALUE ' '.                                   
003000            88 MID-KDCMD-DELETE                                           
003100                             VALUE 'D'                                    
003200                             'B'.                                         
003300            88 MID-KDCMD-REPLACE                                          
003400                             VALUE 'R'                                    
003500                             'Ä'.                                         
003600            88 MID-KDCMD-INSERT                                           
003700                             VALUE 'I'                                    
003800                             'N'.                                         
003900            88 MID-KDCMD-SELECT                                           
004000                             VALUE 'S'                                    
004100                             'V'.                                         
004200*                                 RAD-UPPDATERINGSKOMMANDO                
004300*                                  BLANK  = INGENTING                     
004400*                                  D , B  = DELETE                        
004500*                                  R , Ä  = REPLACE                       
004600*                                  I , N  = INSERT                        
004700*                                  S , V  = SELECT                        
004800           07 MID-IDCATNR    PIC X(5).                                    
004900*                                 KATALOG-ID                              
005000*** END OF VILMAII-COPY LENGTH= 276 BYTES                                 
