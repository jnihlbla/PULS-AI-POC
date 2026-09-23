//W371JME2 JOB (640W3710100W371JME2,W100),'RTN W371D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//WEMPTST EXEC WEMPTST,DSIN=W371.W371D1.W371FEL(0)                              
//        EXEC WMEMOSND,CONDS='(0,LT,WEMPTST.T)',                               
//             REQS=W371D1F1                                                    
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W371JME2                                            
