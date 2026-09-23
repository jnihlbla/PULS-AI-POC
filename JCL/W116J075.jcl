//W116J075 JOB (640W1160100W116J075,W100),'RTN W116S9',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*    EXPEDITER=&VCOM                                                          
//*                                                                             
//W116    EXEC W116P075                                                         
//*                                                                             
//W11675.SYSINPUT DD *                                                          
&VCOM                                                                           
//*                                                                             
//SOPSET  EXEC WSOP                                                             
SET VALUE W116S9                                                                
 VCOM(&VCOM)                                                                    
END-SET                                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116S9.W11675(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
// EXEC WZ14DAP4,DSIN=W116.W116S9.W11675(+1)                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J075                                         
