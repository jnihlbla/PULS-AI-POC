000100 01  RESP-W60362O1.                                                       
000200*                                 RESP-COPYTEXT FÖR W6036200              
000300*                                                                         
000400     03 RESP-IDARTNR-UPD     PIC Z(7)9.                                   
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 RESP-IDTEKINF-UPD    PIC X(180).                                  
000800*                                 TEKNINSK INFO ARTIKEL ETIKETT           
000900*                                 PART LABEL TECHNICAL INFO               
001000     03 RESP-KVRADER         PIC Z(4)9.                                   
001100*                                 ANTAL RADER                             
001200*                                 NUMBER OF LINES                         
001300     03 RESP-LINE            OCCURS 999 TIMES.                            
001400        05 RESP-KDCMD-LINE   PIC X.                                       
001500*                                 RAD-UPPDATERINGSKOMMANDO                
001600*                                  BLANK  = INGENTING                     
001700*                                  D , B  = DELETE                        
001800*                                  R , Ä  = REPLACE                       
001900*                                  I,N,A  = INSERT                        
002000*                                  S , V  = SELECT                        
002100*                                  P , P  = PRINT                         
002200*                                  C , K  = COPY                          
002300*                                 LINE UPDATE COMMAND                     
002400        05 RESP-IDARTNR-LINE PIC Z(7)9.                                   
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700        05 RESP-BEART-LINE   PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900*                                 PART DESCRIPTION                        
003000        05 RESP-IDTEKINF-LINE                                             
003100                             PIC X(180).                                  
003200*                                 TEKNINSK INFO ARTIKEL ETIKETT           
003300*                                 PART LABEL TECHNICAL INFO               
003400*** END OF VILMAII-COPY LENGTH= 213979 BYTES                              
