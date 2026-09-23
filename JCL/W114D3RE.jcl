//W114D3RE JOB (640W1140100W114D3RE,W100),'RTN W114D3',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SPOC      EXEC PGM=CSLUSPOC,                                                  
//  PARM=('IMSPLEX=IMG0,ROUTE=*,WAIT=30')                                       
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
 UPDATE DB NAME(WDF7*) START(ACCESS) SET(ACCTYPE(UPD))                          
//*                                                                             
//FREE    EXEC WFREE,NAME=W114D3,MAXRC=8                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114D3RE                                         
