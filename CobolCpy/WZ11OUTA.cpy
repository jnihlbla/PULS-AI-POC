000100 01  OUTA-WZ11OUT.                                                        
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ11OUTA (TEMP ARCHIVE)         
000400*                                 CALLS:                                  
000500*                                    USING OUTA-WZ11OUT                   
000600*                                                                         
000700*                                 IDCALL=001-9 DEPENDING ON WHICH         
000800*                                  OUTPUT "CHANNEL" IS USED.              
000900*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001000*                                 KDRC=0 IF OK; = 8 IF OPEN FAILS         
001100*                                                                         
001200*                                 IDOUTDEST IS USED TO BUILD THE          
001300*                                 NAME OF THE RESULTING PDF FILE.         
001400*                                 IT SHOULD ONLY CONTAIN CHARAC-          
001500*                                 TERS VALID IN A UNIX FILE NAME.         
001600*                                                                         
001700     03 OUTA-IDCALL          PIC S9(9)           COMP.                    
001800*                                 ID OF CALL SEQUENCE                     
001900     03 OUTA-KDFUNC          PIC X(10).                                   
002000*                                 FUNCTION CODE                           
002100     03 OUTA-KDRC            PIC S9(9)           COMP.                    
002200*                                 RETURN CODE                             
002300     03 OUTA-OPEN-PARMS-FILLER.                                           
002400        05 OUTA-OPEN-PARMS.                                               
002500*                                 FIELDS USED ON OPEN CALL.               
002600           07 OUTA-IDOUTDEST PIC X(60).                                   
002700*                                 PHYSICAL OUTPUT DESTINATION             
002800           07 OUTA-IDPFDEF   PIC X(8).                                    
002900*                                 IBM PSF FORMSDEF,PAGEDEF                
003000           07 OUTA-FLCARRCNTL                                             
003100                             PIC X.                                       
003200*                                 IS CARRRIAGE CONTROL INCLUDED?          
003300           07 OUTA-TIREGDAT  PIC S9(7)           COMP-3.                  
003400*                                 REGISTRATION DATE (YYMMDD)              
003500           07 OUTA-TIREGTID  PIC S9(7)           COMP-3.                  
003600*                                 GENERAL REGISTRATION TIME               
003700           07 OUTA-IDOUTTYPE PIC X(15).                                   
003800*                                 OUTPUT TYPE                             
003900           07 OUTA-IDOUTREC  PIC X(30).                                   
004000*                                 OUTPUT RECEIVER                         
004100           07 OUTA-IDLIST    PIC X(10).                                   
004200*                                 LIST IDENTITY                           
004300           07 OUTA-TIKLOCK   PIC 9(8).                                    
004400*                                 TIME OF DAY (HHMMSSTH)                  
004500        05 FILLER            PIC X(2864).                                 
004600     03 OUTA-PUT-PARAMETERS REDEFINES OUTA-OPEN-PARMS-FILLER.             
004700*                                 FIELDS USED ON PUT CALL                 
004800        05 OUTA-TEOUTDATA-L  PIC S9(9)           COMP.                    
004900*                                 LENGTH OF DATA                          
005000        05 OUTA-TEOUTDATA    PIC X(3000).                                 
005100*                                 OUTPUT DATA                             
005200*** END OF VILMAII-COPY LENGTH= 3022 BYTES                                
