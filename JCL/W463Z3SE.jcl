//W463Z3SE JOB (670W4630100W463Z3SE,W100),'RTN W463S7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=WUT.W463S7.W46340(+0)                               
//*                                                                             
//     IF (EMPTYT.T.RC = 0) THEN                                                
//* ORDER TILL EDI                                                              
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=WUT.W463S7.W46340(+0)                                       
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.EDI.ORDERS                                                  
¤MQMPROP SenderId=BP2T7                                                         
/*                                                                              
//     ELSE                                                                     
//DEL1     EXEC PGM=IEFBR14                                                     
//DD    DD DSN=WUT.W463S7.W46340(+0),DISP=(OLD,DELETE)                          
//     ENDIF                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463Z3SE                                         
