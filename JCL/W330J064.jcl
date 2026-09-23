//W330J064 JOB (650W3300100W330J064,W100),'RTN W330B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(5,0)                                               
/*JOBPARM LINES=999,CARDS=0,FORMS=1800,LINECT=0                                 
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W330P064 EXEC W330P064                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J064                                            
