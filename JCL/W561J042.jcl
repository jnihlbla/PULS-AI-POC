//W561J042 JOB (640W5610100W561J042,W100),'RTN W561V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W561    EXEC W561P042                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W561.W561V1.W56142(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP3,DSIN=W561.W561V1.W56142(+1)                                    
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W561J042                                         
