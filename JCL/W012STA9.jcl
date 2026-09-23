//W012STA9 JOB (650W0020200W012STA9,W100),'RTN W012V9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND D2G0                                                               
//*                                                                             
//***** LÄGG IN START DATABASE FÖR DAG ELLER VECKOBATCHEN                       
//***** NÄR DET BLIR AKTUELLT                                                   
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W012STA9                                         
