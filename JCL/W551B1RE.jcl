//W551B1RE JOB (670W5510100W551B1RE,W100),'RTN W551B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
// EXEC WZ14PDAP                                                                
//SYSIN           DD *                                                          
W551B1-001                                                                      
W551B1                                                                          
//*                                                                             
//SPOC      EXEC PGM=CSLUSPOC,                                                  
//  PARM=('IMSPLEX=IMG0,ROUTE=*,WAIT=30')                                       
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
 UPDATE DB NAME(WDC6*) START(ACCESS) SET(ACCTYPE(UPD))                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W551B1RE                                         
