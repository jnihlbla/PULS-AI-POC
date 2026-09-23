000010*** EDIT ALLOWED                                                          
000100 01  WDSAREA.                                                             
000200*                                 PARAMETERAREA TILL WDSINFO.             
000300*                                 ----- INPARAMETRAR  -----------         
000400*                                 DDNAMN TILL FIL OM VILKEN INFO          
000500*                                 ÖNSKAS.                                 
000600*                                 ----  UTPARAMETRAR  -----------         
000700*                                 RESTERANDE FÄLT ENLIGT COPYTEXT.        
000800*                                 INMATADE ANTALET SIFFROR.               
000900*                                 ----  ANROP  ------------------         
001000*                                 MOVE 'DDNAMN' TO DDNAME                 
001100*                                 CALL WDSINFO USING DSAREA               
001200     03  DDNAME              PIC X(8).                                    
001300*                                 DDNAMN                                  
001400     03  DSNAME              PIC X(44).                                   
001500*                                 DATASET-NAMN                            
001600     03  IDGEN               PIC 9(5).                                    
001700*                                 RELATIVT GENERATIONS-NUMMER             
001800     03  CREDTE              PIC 9(5).                                    
001900*                                 CREATION DATE (IOCS)                    
002000     03  EXPDTE              PIC 9(5).                                    
002100*                                 EXPIRATION DATE (IOCS)                  
002200     03  RECFM               PIC X(3).                                    
002300*                                 RECORD FORMAT                           
002400     03  LRECL               PIC 9(5).                                    
002500*                                 RECORD LÄNGD                            
002600     03  BLKSIZE             PIC 9(5).                                    
002700*                                 BLOCKSTORLEK                            
002800     03  VOLSER              PIC X(6).                                    
002900*                                 FÖRSTA VOLYM DÄR DS LIGGER              
003000     03  KDSVAR              PIC X(1).                                    
003100         88  KDSVAR-OK                       VALUE ' '.                   
003200         88  KDSVAR-FEL                      VALUE 'F'.                   
003300*                                                                         
003400*** END COPY WDSAREACC0  LENGTH=87                                        
