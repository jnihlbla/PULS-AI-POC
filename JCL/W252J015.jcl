//W252J015 JOB (640W2520100W252J015,W100),'RTN W252D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//COPY EXEC W001HFSC,CONV='(BPXFX311)',                                         
//             DSIN=W252.W252D1.W25214(+0),                                     
//             PATHOUT='/app/vccs/qase/w221/data/w25214.xls'                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W252J015                                         
