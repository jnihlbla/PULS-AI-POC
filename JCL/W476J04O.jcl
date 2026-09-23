//W476J04O JOB (540W4760100W476J04O,W100),'RTN W476SP',                         
//             CLASS=L,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  IDSHIPM = &IDSHIPM                                                         
//*                                                                             
//W476    EXEC W476P04O                                                         
//W4764O.W4764OD1 DD *                                                          
&IDSHIPM.                                                                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W476J04O                                         
