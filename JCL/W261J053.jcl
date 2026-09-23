//W261J053 JOB (640W2610100W261J053,W100),'RTN W261V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                        Sends email to                                       
//*                        Carina Jansenius and Lena Mårtensson                 
//******       DSIN=W261.W261V1.W26152(+0),                                     
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W261.W261V1.W26152(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W261.W261V1.W26152(+0)                                    
//SYSIN           DD *                                                          
W26152-001                                                                      
W26152                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W261J053                                         
