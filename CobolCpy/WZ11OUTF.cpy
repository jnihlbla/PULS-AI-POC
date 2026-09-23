000100 01  OUTF-WZ11OUT.                                                        
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ11OUTF (FAX)                  
000400*                                 CALLS:                                  
000500*                                    USING OUTF-WZ11OUT                   
000600*                                                                         
000700*                                 IDCALL=001-9 DEPENDING ON WHICH         
000800*                                  OUTPUT "CHANNEL" IS USED.              
000900*                                  (NORMALLY PCB NAME "OUT*" IS           
001000*                                  USED, BUT AN EXPLICIT PCB NAME         
001100*                                  MAY ALSO BE SPECIFIED IN THE.          
001200*                                  OPEN CALL.)                            
001300*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001400*                                 KDRC=0 IF OK; = 8 IF OPEN FAILS         
001500*                                                                         
001600     03 OUTF-IDCALL          PIC S9(9)           COMP.                    
001700*                                 ID OF CALL SEQUENCE                     
001800     03 OUTF-KDFUNC          PIC X(10).                                   
001900*                                 FUNCTION CODE                           
002000     03 OUTF-KDRC            PIC S9(9)           COMP.                    
002100*                                 RETURN CODE                             
002200     03 OUTF-OPEN-PARMS-FILLER.                                           
002300        05 OUTF-OPEN-PARMS.                                               
002400*                                 FIELDS USED ON OPEN CALL.               
002500           07 OUTF-IDOUTDEST PIC X(60).                                   
002600*                                 PHYSICAL OUTPUT DESTINATION             
002700           07 OUTF-IDPFDEF   PIC X(8).                                    
002800*                                 IBM PSF FORMSDEF,PAGEDEF                
002900           07 OUTF-FLCARRCNTL                                             
003000                             PIC X.                                       
003100*                                 IS CARRRIAGE CONTROL INCLUDED?          
003200           07 OUTF-TEFAX     OCCURS 5 TIMES                               
003300                             PIC X(50).                                   
003400*                                 FAX INFO LINE                           
003500           07 OUTF-IDMAIL-SENDER                                          
003600                             PIC X(60).                                   
003700*                                 ID/PWD OF SENDER                        
003800           07 OUTF-IDOUTTYPE PIC X(15).                                   
003900*                                 OUTPUT TYPE                             
004000           07 OUTF-IDOUTREC  PIC X(30).                                   
004100*                                 OUTPUT RECEIVER                         
004200           07 OUTF-IDLIST    PIC X(10).                                   
004300*                                 LIST IDENTITY                           
004400           07 OUTF-TIREGDAT  PIC 9(6).                                    
004500*                                 REGISTRATION DATE (YYMMDD)              
004600           07 OUTF-TIKLOCK   PIC 9(8).                                    
004700*                                 TIME OF DAY (HHMMSSTH)                  
004800        05 FILLER            PIC X(2556).                                 
004900     03 OUTF-PUT-PARAMETERS REDEFINES OUTF-OPEN-PARMS-FILLER.             
005000*                                 FIELDS USED ON PUT CALL                 
005100        05 OUTF-TEOUTDATA-L  PIC S9(9)           COMP.                    
005200*                                 LENGTH OF DATA                          
005300        05 OUTF-TEOUTDATA    PIC X(3000).                                 
005400*                                 OUTPUT DATA                             
005500*** END OF VILMAII-COPY LENGTH= 3022 BYTES                                
