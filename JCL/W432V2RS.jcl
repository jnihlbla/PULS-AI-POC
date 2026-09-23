//W432V2RS JOB (640W4320100W432V2RS,W100),'RTN W432V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SPOC      EXEC PGM=CSLUSPOC,                                                  
//  PARM=('IMSPLEX=IMG0,ROUTE=*,WAIT=30')                                       
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
 UPDATE DB NAME(WDB7*) STOP(ACCESS)                                             
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=10                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W432V2RS                                         
//*                                                                             
