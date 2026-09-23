//W418J030 JOB (640W4180100W418J030,W100),'RTN W418D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//W418    EXEC W418P030                                                         
//*                                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W418.W418D2.W418AN(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W418.W418D2.W418AN(+1)                                    
//SYSIN           DD *                                                          
W41830-001                                                                      
W41830                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J030                                         
