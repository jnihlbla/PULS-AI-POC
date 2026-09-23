//W418J03B JOB (670W4180100W418J03B,W100),'RTN W418D4',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTZ                                                    
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W418    EXEC W418P03B                                                         
//*                                                                             
//W4183B.W4183BD2 DD DSN=W418.REOR.DAEXDAT7(+1)                                 
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W418.W418D4.W418AY(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W418.W418D4.W418AY(+1)                                    
//SYSIN           DD *                                                          
W4183B-001                                                                      
W4183B                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J03B                                         
