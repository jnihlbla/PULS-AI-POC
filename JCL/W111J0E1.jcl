//W111J0E1 JOB (640W1110100W111J0E1,W100),'RTN W111E1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//*                                                                             
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTSSUPPLIERCOMMCDREQ                                 
¤DSOUT W111.W111X1SE.W11137                                                     
¤RECFM VB                                                                       
¤LRECL 255                                                                      
/*                                                                              
//SOP     EXEC WSOP                                                             
  ORDER W111B1                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111J0E1                                         
