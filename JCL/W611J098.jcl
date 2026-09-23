//W611J098 JOB (640W6110100W611J098,W100),'RTN W611V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST6                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P098                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611V1.W61198(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W611.W611V1.W61198(+1)                                    
//SYSIN           DD *                                                          
W61198-001                                                                      
W61198                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W611J098                                         
