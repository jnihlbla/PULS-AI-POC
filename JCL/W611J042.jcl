//W611J042 JOB (640W6110100W611J042,W100),'RTN W611V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W611    EXEC W611P042                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611V2.W611ME(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W611.W611V2.W611ME(+1)                                    
//SYSIN           DD *                                                          
W61142-001                                                                      
W61142                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J042                                         
