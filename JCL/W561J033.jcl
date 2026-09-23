//W561J033 JOB (640W5610100W561J033,W100),'RTN W561D5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W561     EXEC W561P033                                                        
//*                                                                             
//W56133.SYSUDUMP DD SYSOUT=*                                                   
//W56133.CEEDUMP  DD SYSOUT=*                                                   
//W56133.IDIREPRT DD SYSOUT=(A,,SYST)                                           
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W561.W561D5.W56135(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W561.W561D5.W56135(+1)                                    
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W561J033                                         
