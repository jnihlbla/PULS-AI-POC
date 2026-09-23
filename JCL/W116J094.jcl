//W116J094 JOB (640W1160100W116J094,W100),'RTN W116SG',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W116    EXEC W116P094                                                         
//*                                                                             
//W11694.W11694D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W116J094                                         
