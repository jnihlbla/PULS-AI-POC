000100 01  MID-W4I67501.                                                        
000200*                                 MID-COPYTEXT FÖR W40675                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500*                                 TRANSPORT IDENTITY                      
000600     03 MID-IDLBBET-IN       PIC X(12).                                   
000700*                                 LASTBÄRARBETECKNING                     
000800*                                 TRAILER NUMBER                          
000900     03 MID-FLFARLIG-IN      PIC X.                                       
001000*                                 FARLIGT GODS-FLAGGA                     
001100*                                 DENGEROUS GOODS FLAG                    
001200     03 MID-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 MID-FLSKRIV-NU       PIC X.                                       
001600*                                 J/Y = SKRIV BEGÄRD LISTA                
001700*                                 J/Y = PRINT REPORT NOW                  
001800     03 MID-IDSHIPM          PIC 9(7).                                    
001900*                                 SKEPPNINGSNUMMER                        
002000*                                 SHIPMENT NO                             
002100     03 MID-FLAVSLUTA        PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300*                                 GENERAL FLAG                            
002400     03 MID-INDATA           OCCURS 13 TIMES.                             
002500*                                 MID-COPYTEXT FÖR W40675                 
002600        05 MID-KDCMD         PIC X.                                       
002700         88 MID-KDCMD-INGENTING                                           
002800                             VALUE ' '.                                   
002900         88 MID-KDCMD-DELETE VALUE 'D'                                    
003000                             'B'.                                         
003100         88 MID-KDCMD-REPLACE                                             
003200                             VALUE 'R'                                    
003300                             'Ä'.                                         
003400         88 MID-KDCMD-INSERT VALUE 'I'                                    
003500                             'N'.                                         
003600         88 MID-KDCMD-SELECT VALUE 'S'                                    
003700                             'V'.                                         
003800         88 MID-KDCMD-PRINT  VALUE 'P'                                    
003900                             'P'.                                         
004000*                                 RAD-UPPDATERINGSKOMMANDO                
004100*                                  BLANK  = INGENTING                     
004200*                                  D , B  = DELETE                        
004300*                                  R , Ä  = REPLACE                       
004400*                                  I , N  = INSERT                        
004500*                                  S , V  = SELECT                        
004600*                                  P , P  = PRINT                         
004700*                                 LINE UPDATE COMMAND                     
004800*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
