//W114J0E1 JOB (640W1140100W114J0E1,W100),'RTN W114E1',                         
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
¤ADDISPABS CARPARTS.PULS.PARTINFOFROMKDP                                        
¤DSOUT W114.W114X1SE.W11412                                                     
¤RECFM FB                                                                       
¤LRECL 338                                                                      
/*                                                                              
//ORDER  EXEC WSOP                                                              
    ORDER W114S8                                                                
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J0E1                                         
