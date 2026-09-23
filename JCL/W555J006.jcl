//W555J006 JOB (650W5550100W555J006,W100),'RTN W555B3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W555    EXEC W555P006,                                                        
//             INDUT=WXTR.W555.&IDUSER                                          
//*                                                                             
&IDARTNR &IDDC &HUVTYP &SUBTYP &MAXDAT &MINDAT &IDTRANS &IDUSER &IDLAND         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W555J006                                         
