//W371RAL1 JOB (640W3710100W371RAL1,W100),'RTN W371V9',                         
//             CLASS=K,TIME=(5,0),                                              
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W371.W371V9.W3717A(+0),                           
//             TTLOAD=BYLRADLO,UID=W371RAL1,JOBNAME=W371RAL1                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371RAL1                                         
