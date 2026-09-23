//FIMPROD  JOB (510WOS39000),'ANDRE KJELL',                                     
//             MSGCLASS=H,MSGLEVEL=(2,0),                                       
//             CLASS=N,TIME=1                                                   
/*JOBPARM ROOM=PVV2,LINES=5,CARDS=0,FORMS=STD,LINECT=00                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IMF     EXEC  PGM=IMFSUBEX,PARM='SS(IF1P) EXEC(FMTURG)'                       
//STEPLIB DD DSN=F1IM00.IMF.BBLINK,DISP=SHR                                     
