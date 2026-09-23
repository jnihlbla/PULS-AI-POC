//W473Z1SE JOB (670W4730100W473Z1SE,W100),'RTN W473D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* PACKAGING FILE TO TMS                                                       
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=WUT.W473D1.W47393(+0)                               
//*                                                                             
//   IF (EMPTYT.T.RC = 4) THEN                                                  
//DEL1  EXEC PGM=IEFBR14                                                        
//DD    DD DSN=WUT.W473D1.W47393(+0),DISP=(OLD,DELETE)                          
//   ELSE                                                                       
//VCOM    EXEC W016P022,VCOM=W473Z1SE                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W473D1.W47393(+0),DISP=SHR                         
//*                                                                             
//   ENDIF                                                                      
//SOPEND  EXEC WSOPEND,PROCESS=W473Z1SE                                         
