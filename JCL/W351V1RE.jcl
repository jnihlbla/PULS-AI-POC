//W351V1RE JOB (670W3510100W351V1RE,W100),'RTN W351V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM FORMS=1800,LINECT=0,LINES=9                                           
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE  XEQ   LOCAL                                                            
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W351V1,MAXRC=8                                        
//*                                                                             
//SPOC      EXEC PGM=CSLUSPOC,                                                  
//  PARM=('IMSPLEX=IMG0,ROUTE=*,WAIT=30')                                       
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
 UPDATE DB NAME(WDK8*) START(ACCESS) SET(ACCTYPE(UPD))                          
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W351V1RE                                         
