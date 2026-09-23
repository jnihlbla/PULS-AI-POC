//WFSG4DD1 JOB (650W3300100WFSG4DD1,W100),'RTN W330R1',                         
//             CLASS=K,TIME=(5,0),                                              
//             USER=W0SOP01,PASSWORD=?                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//TERM    EXEC SQLBATCH,SQLINP=NULLFILE                                         
//SYSTSIN DD *                                                                  
 PROFILE PREFIX(PC69097)                                                        
 DSN SYSTEM(D2G0)                                                               
-TERM UTILITY(WFSG4DD1)                                                         
//**SOP     EXEC WSOPEND,PROCESS=WFSG4DD1                                       
