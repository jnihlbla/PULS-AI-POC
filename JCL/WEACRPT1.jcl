//WEACRPT1 JOB (540W0030200WEACRPT1,W100),'RTN WEACD1',                         
//             MSGCLASS=A,CLASS=K,USER=?,PASSWORD=?                             
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*                                                                             
//* EXTRACT DATA FROM THE APPLICATION LOG FILES                                 
//*                                                                             
//PREPARE EXEC FLOGON                                                           
//SYSEXEC  DD  DSN=WEAC.PUBLIC.EXEC                                             
//SYSTSIN  DD  *                                                                
 %EACREPRT WEAC.QASE PULS 0-3                                                   
//REPRTDD  DD  DSN=&&DATA,DISP=(NEW,PASS),LRECL=250,RECFM=FB                    
//*                                                                             
//*                                                                             
//* FORMAT A REPORT FROM THE EXTRACTED DATA                                     
//*                                                                             
//PRINT   EXEC VEZCG000,MACLIB1='WEAC.PUBLIC.EPLUSCPY'                          
//INDD     DD  DSN=&&DATA,DISP=(OLD,DELETE)                                     
//OUTDD    DD  DSN=&&REPORT,DISP=(NEW,PASS)                                     
PARM DEBUG STATE                                                                
FILE INDD                                                                       
%EACREPRT                                                                       
*                                                                               
FILE OUTDD PRINTER FB(120 0)                                                    
*                                                                               
JOB INPUT INDD                                                                  
IF OPENDURATION > 5 AND CLOSEDATE = ' '                                         
  PRINT R1                                                                      
END-IF                                                                          
*                                                                               
REPORT R1 PRINTER OUTDD LINESIZE 100 NOADJUST NOPAGE NODATE                     
SEQUENCE OPENDATE                                                               
TITLE 01    ' EAC ids open for more than 5 days '                               
HEADING OPENDATE   ('Opened' 'on')                                              
*EADING SUSPDATE   ('Suspended' 'on')                                           
*EADING UNSUDURATION ('Unsuspended' 'hours')                                    
*EADING CLOSEDATE  ('Closed' 'on')                                              
HEADING OPENDURATION ('Open' 'days')                                            
HEADING TICKET     ('Ticket')                                                   
HEADING EACID      ('Emer.' 'Id')                                               
HEADING OPENID     ('Opened' 'by')                                              
HEADING OPENMAIL   ('Opener''s' 'mail id')                                      
*EADING DOCID      ('eTracker')                                                 
LINE OPENDATE OPENDURATION TICKET EACID OPENID OPENMAIL                         
//*                                                                             
//CHECK   EXEC WEMPTST,DSIN=&&REPORT                                            
//EMPTEST IF  (CHECK.T.RC EQ 0) THEN                                            
//SEND    EXEC WMEMOSND                                                         
//APIFILE  DD  *                                                                
)SEND                                                                           
ETITLE EAC ids open for more than 5 days                                        
DEST henrik.dahlbom@volvocars.com                                               
DEST susanne.samuelsson@volvocars.com                                           
DEST rahul.reddy@volvocars.com                                                  
MEMO MAILDATA                                                                   
)END                                                                            
//MAILDATA DD  DSN=&&REPORT,DISP=(OLD,DELETE)                                   
//EMPTEST ENDIF                                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WEACRPT1                                         
