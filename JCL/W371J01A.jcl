//W371J01A JOB (640W3710100W371J01A,W100),'RTN W371D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*                                                                             
//WEMPTST EXEC WEMPTST,DSIN=W371.W371D1.W371FE2(0)                              
//        EXEC WMEMOSND,CONDS='(0,LT,WEMPTST.T)',                               
//             REQS=W371D1F2                                                    
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W371J01A                                            
