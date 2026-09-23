000100 01  OUTM-WZ11OUT.                                                        
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ11OUTM (E-MAIL)               
000400*                                 CALLS:                                  
000500*                                    USING OUTM-WZ11OUT                   
000600*                                                                         
000700*                                 IDCALL=001-9 DEPENDING ON WHICH         
000800*                                  OUTPUT "CHANNEL" IS USED.              
000900*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001000*                                 KDRC=0 IF OK; = 8 IF OPEN FAILS         
001100*                                                                         
001200     03 OUTM-IDCALL          PIC S9(9)           COMP.                    
001300*                                 ID OF CALL SEQUENCE                     
001400     03 OUTM-KDFUNC          PIC X(10).                                   
001500*                                 FUNCTION CODE                           
001600     03 OUTM-KDRC            PIC S9(9)           COMP.                    
001700*                                 RETURN CODE                             
001800     03 OUTM-OPEN-PARMS-FILLER.                                           
001900        05 OUTM-OPEN-PARMS.                                               
002000*                                 FIELDS USED ON OPEN CALL.               
002100           07 OUTM-IDOUTDEST PIC X(60).                                   
002200*                                 PHYSICAL OUTPUT DESTINATION             
002300           07 OUTM-IDPFDEF   PIC X(8).                                    
002400*                                 IBM PSF FORMSDEF,PAGEDEF                
002500           07 OUTM-FLCARRCNTL                                             
002600                             PIC X.                                       
002700*                                 IS CARRRIAGE CONTROL INCLUDED?          
002800           07 OUTM-TEFAX     OCCURS 5 TIMES                               
002900                             PIC X(50).                                   
003000*                                 FAX INFO LINE                           
003100           07 OUTM-IDMAIL-SENDER                                          
003200                             PIC X(60).                                   
003300*                                 ID/PWD OF SENDER                        
003400           07 OUTM-IDMAILTTL PIC X(250).                                  
003500*                                 E-MAIL TITLE                            
003600           07 OUTM-IDOUTTYPE PIC X(15).                                   
003700*                                 OUTPUT TYPE                             
003800           07 OUTM-IDOUTREC  PIC X(30).                                   
003900*                                 OUTPUT RECEIVER                         
004000           07 OUTM-IDLIST    PIC X(10).                                   
004100*                                 LIST IDENTITY                           
004200           07 OUTM-TIREGDAT  PIC 9(6).                                    
004300*                                 REGISTRATION DATE (YYMMDD)              
004400           07 OUTM-TIKLOCK   PIC 9(8).                                    
004500*                                 TIME OF DAY (HHMMSSTH)                  
004600        05 FILLER            PIC X(2306).                                 
004700     03 OUTM-PUT-PARAMETERS REDEFINES OUTM-OPEN-PARMS-FILLER.             
004800*                                 FIELDS USED ON PUT CALL                 
004900        05 OUTM-TEOUTDATA-L  PIC S9(9)           COMP.                    
005000*                                 LENGTH OF DATA                          
005100        05 OUTM-TEOUTDATA    PIC X(3000).                                 
005200*                                 OUTPUT DATA                             
005300*** END OF VILMAII-COPY LENGTH= 3022 BYTES                                
