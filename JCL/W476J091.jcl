//W476J091 JOB (540W4760100W476J091,W100),'RTN W476S5',                         
//             CLASS=V,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W476    EXEC W476P091                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W476J091                                         
