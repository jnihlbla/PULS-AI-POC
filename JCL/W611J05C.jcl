//W611J05C JOB (640W6110100W611J05C,W100),'RTN W611V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W611    EXEC W611P05C                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611V1.W6115C(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W611.W611V1.W6115C(+1)                                    
//SYSIN           DD *                                                          
W6115C-001                                                                      
W6115C                                                                          
//    ENDIF                                                                     
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W611J05C                                         
