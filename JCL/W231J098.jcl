//W231J098 JOB (650W2310100W231J098,W100),'RTN W231S3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P098                                                         
//*                                                                             
//W23198.W23198D4 DD *                                                          
&IDUSER.&URVAL.&DC.                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231J098                                         
