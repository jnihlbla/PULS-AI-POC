//W551B1RS JOB (670W5510100W551B1RS,W100),'RTN W551B1',                         
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
 UPDATE DB NAME(WDC6*) STOP(ACCESS)                                             
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=10                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551B1RS                                         
//*                                                                             
