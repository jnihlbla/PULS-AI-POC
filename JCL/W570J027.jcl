//W570J027 JOB (640W5700100W570J027,W100),'RTN W570D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W570    EXEC W570P027                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570D3.W5702Q(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W570.W570D3.W5702Q(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570D3.W5702R(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP4,DSIN=W570.W570D3.W5702R(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W570J027                                         
