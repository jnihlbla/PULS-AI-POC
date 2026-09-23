//W463J099 JOB (650W4630100W463J099,W100),'RTN W463S7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE PRINT LOCAL                                                             
/*ROUTE XEQ   LOCAL                                                             
//*                                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463S7.W46341(+0)                              
//*                                                                             
//     IF (EMPTYT.T.RC = 0) THEN                                                
//IDCAMS EXEC PGM=IDCAMS                                                        
//SYSPRINT DD SYSOUT=*                                                          
  DELETE (W802.TR.EDI) NONVSAM PURGE                                            
//*                                                                             
// EXEC WEZTP,MACLIB1=W8.IC.EPLUSCPY,                                           
//             MACLIB2=W.QASE.EPLUSCPY,                                         
//             SORTCYL=10                                                       
//EZTP.EZTVFM DD UNIT=WD,SPACE=(CYL,(50,10),RLSE)                               
*                                                                               
*                                                                               
PARM DEBUG (FLOW)                                                               
*                                                                               
LIST ON NOMACROS                                                                
*                                                                               
FILE AAA   FB(121 0) PRINTER                                                    
*                                                                               
*                                                                               
FILE INFIL                                                                      
%W46341                                                                         
*                                                                               
*                                                                               
FILE SORTFIL FB (200 0) VIRTUAL                                                 
%W46341   'SRT-'                                                                
*                                                                               
*                                                                               
*********'                                                                      
*                                                                               
SORT INFIL TO SORTFIL USING (IDLEVNR IDPRODNR)                                  
*                                                                               
**********                                                                      
*                                                                               
JOB INPUT (SORTFIL KEY(SRT-IDLEVNR SRT-IDPRODNR))                               
*                                                                               
IF SORTFIL                                                                      
 IF NOT DUPLICATE SORTFIL                                                       
  PRINT RAPPORT1                                                                
 END-IF                                                                         
*                                                                               
 IF FIRST-DUP SORTFIL                                                           
  PRINT RAPPORT1                                                                
 END-IF                                                                         
*                                                                               
END-IF                                                                          
*                                                                               
*                                                                               
*                                                                               
REPORT RAPPORT1 SUMMARY NOADJUST NOSPREAD LINESIZE 120 PRINTER AAA              
SEQUENCE SRT-DAREGDAT D SRT-IDLEVNR                                             
CONTROL  SRT-DAREGDAT NOPRINT NEWPAGE +                                         
         SRT-IDLEVNR                                                            
TITLE 1 '   '  COL 15 SYSTIME                                                   
TITLE 2 'ORDERS RELEASED FROM PULS FOR EDI-TRANSMISSION'                        
TITLE 3 'NUMBER OF ON PRODNO-SUPPLIER LEVEL'                                    
TITLE 4 '                 '                                                     
TITLE 5 'IF THE NUMBER DOES NOT MATCH THE COUNT OF'                             
TITLE 6 'ORDERS AT THE SUPPLIER. PLEASE CONTACT THE EDI HELPDESK'               
*                                                                               
LINE 1 +                                                                        
SRT-DAREGDAT +                                                                  
SRT-IDLEVNR +                                                                   
TALLY                                                                           
*                                                                               
*                                                                               
//EZTP.INFIL DD DSN=W463.W463S7.W46341(+0),DISP=SHR                             
//AAA DD DSN=W802.TR.EDI,                                                       
//        MGMTCLAS=NOBACKUP,                                                    
//        DISP=(NEW,CATLG,DELETE),                                              
//        SPACE=(CYL,(1,1),RLSE)                                                
//*                                                                             
//     EXEC WMEMOSND,                                                           
//             DSIN=W802.TR.EDI                                                 
//APIFILE DD *                                                                  
)SEND                                                                           
OPTION FORCE                                                                    
OPTION NOEXPRL                                                                  
OPTION NOSNDREF                                                                 
OPTION NOTRUNC                                                                  
FORMAT TEXT                                                                     
LINESIZE 121                                                                    
TITLE EDI KOLL                                                                  
DEST AUTOPLASTICS.RAUFOSS(A)PLASTAL.COM                                         
MEMO SEND                                                                       
)END                                                                            
//*                                                                             
//*                                                                             
//     ELSE                                                                     
//BBB    EXEC WMEMOSND                                                          
//M.APIFILE  DD *                                                               
)SEND                                                                           
TITLE EDI TOMT                                                                  
OPTION FORCE                                                                    
DEST AUTOPLASTICS.RAUFOSS(A)PLASTAL.COM                                         
MEMO                                                                            
                                                                                
  OBS - INGA ORDERS FRÅN PULS - OBS                                             
                                                                                
)END                                                                            
//M.SEND     DD DUMMY                                                           
//     ENDIF                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463J099                                         
