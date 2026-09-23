//W476J040 JOB (540W4760100W476J040,W100),'RTN W476ST',                         
//             CLASS=L,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  URVAL = &URVAL                                                             
//*                                                                             
//W476    EXEC W476P040                                                         
//W47640.W47640D1 DD *                                                          
&URVAL.                                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W476J040                                         
