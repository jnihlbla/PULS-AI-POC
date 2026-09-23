//W371ARL1 JOB (640W3710100W371ARL1,W100),'RTN W371V9',                         
//             CLASS=K,TIME=(5,0),                                              
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//LOAD     EXEC WG02LOAD,DSIN=W371.W371V9.W3717B(+0),                           
//             TTLOAD=BYLARTLO,UID=W371ARL1,JOBNAME=W371ARL1                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371ARL1                                         
